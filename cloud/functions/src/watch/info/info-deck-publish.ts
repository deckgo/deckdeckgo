import * as functions from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import * as nodemailer from 'nodemailer';

import {DeckData} from '../../model/deck';
import * as Mail from 'nodemailer/lib/mailer';
import {Resources} from '../../utils/resources';

export async function infoDeckPublish(change: functions.Change<DocumentSnapshot>) {
    const newValue: DeckData = change.after.data() as DeckData;

    const previousValue: DeckData = change.before.data() as DeckData;

    if (!newValue || !newValue.meta || !newValue.meta.published || !newValue.meta.pathname) {
        return;
    }

    if (!newValue.owner_id || newValue.owner_id === undefined || newValue.owner_id === '') {
        return;
    }

    const publish: boolean = await isFirstTimePublished(previousValue, newValue);

    if (!publish) {
        return;
    }

    try {
        await sendInfo(change.after.id, newValue);
    } catch (err) {
        console.error(err);
    }
}

function isFirstTimePublished(previousValue: DeckData, newValue: DeckData): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
        if (!previousValue || !newValue) {
            resolve(false);
            return;
        }

        resolve(!previousValue.meta && newValue.meta && newValue.meta.published);
    });
}

function sendInfo(deckId: string, deckData: DeckData): Promise<string> {
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

                deckUrl = Resources.Constants.PRESENTATION.URL + pathname;
            }

            const mailOptions = {
                from: mailFrom,
                to: mailTo,
                subject: 'DeckDeckGo: New published deck',
                html: `<h2>${deckData.meta ? deckData.meta.title : 'Unknown title'}</h2>
                <p>Deck: ${deckId}</p>
                <p>Url: <a href="${deckUrl}">${deckUrl}</a></p>
            `
            };

            const transporter: Mail = nodemailer.createTransport({
                host: mailHost,
                port: 587,
                secure: false, // STARTTLS
                auth: {
                    type: 'LOGIN',
                    user: mailFrom,
                    pass: mailPwd
                }
            });

            await transporter.sendMail(mailOptions);

            resolve();
        } catch (err) {
            reject(err);
        }
    });
}
