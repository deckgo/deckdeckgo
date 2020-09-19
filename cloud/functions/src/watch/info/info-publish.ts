import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';
import * as functions from 'firebase-functions';

import * as nodemailer from 'nodemailer';
import * as Mail from 'nodemailer/lib/mailer';

import {Deck, DeckData} from '../../model/data/deck';
import {TaskData} from '../../model/data/task';

import {findDeck} from '../../utils/data/deck-utils';

export async function infoPublish(snap: DocumentSnapshot, _context: functions.EventContext) {
  const infoMailSkip: string = functions.config().info.mail.skip;

  if (infoMailSkip === 'true') {
    return;
  }

  const task: TaskData = snap.data() as TaskData;

  if (!task || task === undefined || !task.deckId) {
    console.error('Task for info not provided.');
    return;
  }

  const deck: Deck = await findDeck(task.deckId);

  if (!deck || !deck.data) {
    console.error('Deck for info not found.');
    return;
  }

  const publish: boolean = await isFirstTimePublished(task, deck.data);

  if (!publish) {
    return;
  }

  try {
    await sendInfo(task.deckId, deck.data, task);
  } catch (err) {
    console.error(err);
  }
}

async function isFirstTimePublished(task: TaskData, deckData: DeckData): Promise<boolean> {
  if (!task || !deckData) {
    return false;
  }

  const taskCreateAt: Date | null = getDateObj(task.created_at);
  const deckPublishedAt: Date | null = deckData.meta !== undefined && deckData.meta.published ? getDateObj(deckData.meta.published_at) : null;

  if (!taskCreateAt || !deckPublishedAt) {
    return false;
  }

  return taskCreateAt.getTime() < deckPublishedAt.getTime();
}

function getDateObj(myDate: any): Date | null {
  if (myDate === null) {
    return null;
  }

  if (myDate instanceof String || typeof myDate === 'string') {
    return new Date('' + myDate);
  }

  // A Firebase Timestamp format
  if (myDate && typeof myDate.toDate === 'function' && (myDate.seconds >= 0 || myDate.seconds < 0) && (myDate.nanoseconds >= 0 || myDate.nanoseconds < 0)) {
    return new Date(myDate.toDate());
  }

  // A Firebase Timestamp format which for some reason has no mapped function
  if (myDate && (myDate.seconds >= 0 || myDate.seconds < 0) && (myDate.nanoseconds >= 0 || myDate.nanoseconds < 0)) {
    return new Date(myDate.seconds * 1000);
  }

  return myDate;
}

function sendInfo(deckId: string, deckData: DeckData, task: TaskData): Promise<string> {
  return new Promise<string>(async (resolve, reject) => {
    try {
      const mailFrom: string = functions.config().info.mail.from;
      const mailPwd: string = functions.config().info.mail.pwd;
      const mailTo: string = functions.config().info.mail.to;
      const mailHost: string = functions.config().info.mail.host;

      let deckUrl: string = 'Unknown';
      if (deckData && deckData.meta && deckData.meta.pathname) {
        let pathname: string = deckData.meta.pathname;
        pathname += pathname.endsWith('/') ? '' : '/';

        const presentationUrl: string = functions.config().deckdeckgo.presentation.url;

        deckUrl = presentationUrl + pathname;
      }

      const mailOptions = {
        from: mailFrom,
        to: mailTo,
        subject: 'DeckDeckGo: New published deck',
        html: `<h2>${deckData.meta ? deckData.meta.title : 'Unknown title'}</h2>
                <p>Deck: ${deckId}</p>
                <p>Type: ${task.type}</p>
                <p>Url: <a href="${deckUrl}">${deckUrl}</a></p>
            `,
      };

      const transporter: Mail = nodemailer.createTransport({
        host: mailHost,
        port: 587,
        secure: false, // STARTTLS
        auth: {
          type: 'LOGIN',
          user: mailFrom,
          pass: mailPwd,
        },
      });

      await transporter.sendMail(mailOptions);

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}
