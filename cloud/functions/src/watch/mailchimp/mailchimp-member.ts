// Inspired by https://github.com/firebase/extensions/blob/master/auth-mailchimp-sync/functions/src/index.ts

// Mailchimp API: // https://mailchimp.com/developer/reference/lists/list-members/

import * as admin from 'firebase-admin';
import * as functions from 'firebase-functions';

import * as crypto from 'crypto';

// @ts-ignore incorrect typescript typings
import * as Mailchimp from 'mailchimp-api-v3';

export async function createMailchimpMember(userRecord: admin.auth.UserRecord) {
    if (!userRecord || !userRecord.email || userRecord.email === undefined || userRecord.email === '') {
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

    try {
        await doDeleteMailchimpMember(userRecord);
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

            await mailchimp.post(
                `/lists/${mailchimpAudienceId}/members`,
                {
                    email_address: userRecord.email,
                    status: 'subscribed',
                }
            );

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

            await mailchimp.delete(
                `/lists/${mailchimpAudienceId}/members/${hashed}`
            );

            resolve();
        } catch (err) {
            reject(err);
        }
    });
}
