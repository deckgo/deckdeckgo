import * as admin from 'firebase-admin';
import * as functions from 'firebase-functions';

export async function verifyToken(request: functions.Request, acceptAnonymous: boolean = false): Promise<boolean> {
  if (!request.headers.authorization) {
    return false;
  }

  try {
    const token: string = request.headers.authorization.replace(/^Bearer\s/, '');

    const payload: admin.auth.DecodedIdToken = await admin.auth().verifyIdToken(token);

    return payload !== null && (acceptAnonymous || payload.firebase.sign_in_provider !== 'anonymous');
  } catch (err) {
    return false;
  }
}
