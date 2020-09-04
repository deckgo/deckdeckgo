import * as functions from 'firebase-functions';

import * as cors from 'cors';

import {verifyToken} from './utils/request-utils';
import {publishDeck} from './publish/publish-deck';

export async function publishJob(request: functions.Request, response: functions.Response<any>) {
  const corsHandler = cors({origin: true});

  corsHandler(request, response, async () => {
    const validToken: boolean = await verifyToken(request);

    if (!validToken) {
      response.status(400).json({
        error: 'Not Authorized',
      });
      return;
    }

    try {
      await publishDeck(request);

      response.json({
        result: 'success',
      });
    } catch (err) {
      response.status(500).json({
        error: err,
      });
    }
  });
}
