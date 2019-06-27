import {Change} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import * as admin from 'firebase-admin';

import * as puppeteer from 'puppeteer';

import {DeckData} from '../model/deck';
import {Resources} from '../utils/resources';

export async function generateDeckScreenshot(change: Change<DocumentSnapshot>) {
    const newValue: DeckData = change.after.data() as DeckData;

    const previousValue: DeckData = change.before.data() as DeckData;

    if (!newValue || !newValue.meta || !newValue.meta.published || !newValue.meta.pathname) {
        return;
    }

    const update: boolean = await needScreenshot(previousValue, newValue);

    if (!update) {
        return;
    }

    try {
        const imageBuffer: string = await generateScreenshot(newValue);
        await saveScreenshot(newValue, imageBuffer);
    } catch (err) {
        console.error(err);
    }
}

function needScreenshot(previousValue: DeckData, newValue: DeckData): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
        if (!previousValue || !newValue) {
            resolve(false);
            return;
        }

        let firstTimePublished: boolean = false;
        if (!previousValue.meta && newValue.meta && newValue.meta.published) {
            firstTimePublished = true;
        }

        let updated: boolean = false;
        if (previousValue.meta && newValue.meta) {

            const previousPuslishedAt: Date | null = getDateObj(previousValue.meta.updated_at);
            const newPuslishedAt: Date | null = getDateObj(newValue.meta.updated_at);

            if (previousPuslishedAt && newPuslishedAt && newValue.meta.published) {
                updated = previousPuslishedAt.getTime() < newPuslishedAt.getTime();
            }
        }

        resolve(firstTimePublished || updated);
    });
}

function getDateObj(myDate: any): Date | null {
    if (myDate == null) {
        return null;
    }

    if (myDate instanceof String || typeof myDate === 'string') {
        return new Date('' + myDate);
    }

    // A Firebase Timestamp format
    if (myDate && (myDate.seconds >= 0 || myDate.seconds < 0) && (myDate.nanoseconds >= 0 || myDate.nanoseconds < 0)) {
        return new Date(myDate.toDate());
    }

    return myDate;
}

function generateScreenshot(deckData: DeckData): Promise<string> {
    return new Promise<string>(async (resolve, reject) => {
        try {
            if (!deckData || !deckData.meta) {
                reject('No deck or data');
                return;
            }

            const browser = await puppeteer.launch({args: ['--no-sandbox']});

            const page = await browser.newPage();

            // Screenshot size
            await page.setViewport({width: 1024, height: 576});

            await page.goto(Resources.Constants.PRESENTATION_URL + deckData.meta.pathname);

            await (page as any)._client.send('ServiceWorker.enable');
            await (page as any)._client.send('ServiceWorker.stopAllWorkers');

            // Wait for the components/js elements to be loaded
            await page.waitForFunction('document.querySelector("deckgo-deck  > *")');

            const imageBuffer: string = await page.screenshot();

            await browser.close();

            resolve(imageBuffer);
        } catch (err) {
            reject(err);
        }
    });
}

function saveScreenshot(deckData: DeckData, imageBuffer: string): Promise<string> {
    return new Promise<string>(async (resolve, reject) => {
        if (!imageBuffer || imageBuffer === undefined || imageBuffer === '') {
            reject('No screenshot image');
            return;
        }

        if (!deckData || !deckData.meta) {
            reject('No deck or data');
            return;
        }

        const bucket = admin.storage().bucket();

        // TODO
        const file = bucket.file('test.png');

        try {
            await file.save(imageBuffer);

            resolve();
        } catch (err) {
            reject(err);
        }
    });
}
