// Inspired by https://github.com/firebase/extensions/blob/master/auth-mailchimp-sync/functions/src/index.ts

// Mailchimp API: // https://mailchimp.com/developer/reference/lists/list-members/

import * as crypto from 'crypto';
import * as admin from 'firebase-admin';
import * as functions from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';
// @ts-ignore incorrect typescript typings
import * as Mailchimp from 'mailchimp-api-v3';
import {UserData} from '../../model/data/user';

export async function createMailchimpMember(userRecord: admin.auth.UserRecord) {
  if (!userRecord || !userRecord.email || userRecord.email === undefined || userRecord.email === '') {
    return;
  }

  const mailchimpSkip: string = functions.config().mailchimp.skip;

  if (mailchimpSkip === 'true') {
    return;
  }

  try {
    await postMailchimpMember(userRecord);
  } catch (err) {
    console.error(err);
  }
}

export async function deleteMailchimpMember(userRecord: admin.auth.UserRecord) {
  if (!userRecord || !userRecord.email || userRecord.email === undefined || userRecord.email === '') {
    return;
  }

  const mailchimpSkip: string = functions.config().mailchimp.skip;

  if (mailchimpSkip === 'true') {
    return;
  }

  try {
    await doDeleteMailchimpMember(userRecord);
  } catch (err) {
    console.error(err);
  }
}

export async function updateMailchimpMember(change: functions.Change<DocumentSnapshot>) {
  const newValue: UserData = change.after.data() as UserData;

  const previousValue: UserData = change.before.data() as UserData;

  if (!newValue || !newValue.email || newValue.email === undefined || newValue.email === '') {
    return;
  }

  if (newValue && newValue.anonymous) {
    return;
  }

  if (newValue && previousValue && previousValue.newsletter === newValue.newsletter && !previousValue.anonymous) {
    return;
  }

  const mailchimpSkip: string = functions.config().mailchimp.skip;

  if (mailchimpSkip === 'true') {
    return;
  }

  try {
    await putMailchimpMember(newValue);
  } catch (err) {
    console.error(err);
  }
}

function postMailchimpMember(userRecord: admin.auth.UserRecord): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const mailchimpApiKey: string = functions.config().mailchimp.key;
      const mailchimpAudienceId: string = functions.config().mailchimp.audience;

      if (!mailchimpApiKey || !mailchimpAudienceId) {
        reject('Mailchimp configuration not defined.');
        return;
      }

      const mailchimp: Mailchimp = new Mailchimp(mailchimpApiKey);

      await mailchimp.post(`/lists/${mailchimpAudienceId}/members`, {
        email_address: userRecord.email,
        status: 'subscribed',
      });

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}

function doDeleteMailchimpMember(userRecord: admin.auth.UserRecord): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const mailchimpApiKey: string = functions.config().mailchimp.key;
      const mailchimpAudienceId: string = functions.config().mailchimp.audience;

      if (!mailchimpApiKey || !mailchimpAudienceId) {
        reject('Mailchimp configuration not defined.');
        return;
      }

      const hashed = crypto
        .createHash('md5')
        .update(userRecord.email as string)
        .digest('hex');

      const mailchimp: Mailchimp = new Mailchimp(mailchimpApiKey);

      await mailchimp.delete(`/lists/${mailchimpAudienceId}/members/${hashed}`);

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}

function putMailchimpMember(newValue: UserData): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const mailchimpApiKey: string = functions.config().mailchimp.key;
      const mailchimpAudienceId: string = functions.config().mailchimp.audience;

      if (!mailchimpApiKey || !mailchimpAudienceId) {
        reject('Mailchimp configuration not defined.');
        return;
      }

      const hashed = crypto
        .createHash('md5')
        .update(newValue.email as string)
        .digest('hex');

      const mailchimp: Mailchimp = new Mailchimp(mailchimpApiKey);

      await mailchimp.put(`/lists/${mailchimpAudienceId}/members/${hashed}`, {
        email_address: newValue.email,
        status_if_new: 'subscribed',
        status: newValue.newsletter ? 'subscribed' : 'unsubscribed',
      });

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}
