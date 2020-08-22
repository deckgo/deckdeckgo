import {Change} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';
import * as admin from 'firebase-admin';

import fetch, {Response} from 'node-fetch';

import {DeckData} from '../../model/deck';
import {Token, TokenData} from '../../model/token';

import {GitHubForkResponse} from '../../types/github';

import {isDeckPublished} from '../screenshot/utils/update-deck';

export async function publishToGitHub(change: Change<DocumentSnapshot>) {
  const newValue: DeckData = change.after.data() as DeckData;

  const previousValue: DeckData = change.before.data() as DeckData;

  if (!newValue || !newValue.meta || !newValue.meta.published || !newValue.meta.pathname) {
    return;
  }

  if (!newValue.owner_id || newValue.owner_id === undefined || newValue.owner_id === '') {
    return;
  }

  const update: boolean = await isDeckPublished(previousValue, newValue);

  if (!update) {
    return;
  }

  try {
    const token: Token = await findToken(newValue.owner_id);

    if (!token || !token.data || !token.data.github || !token.data.github.token) {
      return;
    }

    await forkRepo(token.data.github.token);
  } catch (err) {
    console.error(err);
  }
}

function findToken(userId: string): Promise<Token> {
  return new Promise<Token>(async (resolve, reject) => {
    try {
      const snapshot: admin.firestore.DocumentSnapshot = await admin.firestore().doc(`/tokens/${userId}/`).get();

      if (!snapshot.exists) {
        reject('Token not found');
        return;
      }

      const tokenData: TokenData = snapshot.data() as TokenData;

      resolve({
        id: snapshot.id,
        ref: snapshot.ref,
        data: tokenData,
      });
    } catch (err) {
      reject(err);
    }
  });
}

function forkRepo(githubToken: string): Promise<GitHubForkResponse> {
  return new Promise<GitHubForkResponse>(async (resolve, reject) => {
    try {
      const githubApiV3: string = 'https://api.github.com/repos';

      const rawResponse: Response = await fetch(`${githubApiV3}/deckgo/deckdeckgo-starter/forks`, {
        method: 'POST',
        headers: {
          Accept: 'application/json',
          'Content-Type': 'application/json',
          Authorization: `token ${githubToken}`,
        },
      });

      if (!rawResponse || !rawResponse.ok) {
        console.error(rawResponse);
        reject(new Error('Error forking the repo.'));
        return;
      }

      const results: GitHubForkResponse = await rawResponse.json();

      if (!results) {
        reject(new Error('Error no response when forking.'));
        return;
      }

      resolve(results);
    } catch (err) {
      console.error('Unexpected error forking the repo.');
      reject(err);
    }
  });
}
