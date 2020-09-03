import * as functions from 'firebase-functions';

import {verifyToken} from '../utils/request-utils';

export async function publishDeck(request: functions.Request, response: functions.Response<any>) {
  const validToken: boolean = await verifyToken(request);

  if (!validToken) {
    response.status(400).json({
      error: 'Not Authorized',
    });
    return;
  }

  try {
    response.json({
      result: 'success',
    });
  } catch (err) {
    response.status(500).json({
      error: err,
    });
  }
}
