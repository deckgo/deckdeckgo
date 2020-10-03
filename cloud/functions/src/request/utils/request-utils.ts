import * as admin from 'firebase-admin';
import * as functions from 'firebase-functions';

export async function verifyToken(request: functions.Request, acceptAnonymous: boolean = false): Promise<boolean> {
  try {
    const token: string | undefined = await getToken(request);

    if (!token) {
      return false;
    }

    const payload: admin.auth.DecodedIdToken = await admin.auth().verifyIdToken(token);

    return payload !== null && (acceptAnonymous || payload.firebase.sign_in_provider !== 'anonymous');
  } catch (err) {
    return false;
  }
}

export async function getToken(request: functions.Request): Promise<string | undefined> {
  if (!request.headers.authorization) {
    return undefined;
  }

  const token: string = request.headers.authorization.replace(/^Bearer\s/, '');

  return token;
}
