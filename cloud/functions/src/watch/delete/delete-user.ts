import * as admin from 'firebase-admin';

export async function deleteUser(userRecord: admin.auth.UserRecord) {
    if (!userRecord || !userRecord.uid || userRecord.uid === undefined || userRecord.uid === '') {
        return;
    }

    try {
        const userId: string = userRecord.uid;

        await deleteUserDocument(userId)
    } catch (err) {
        console.error(err);
    }
}

function deleteUserDocument(id: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
        try {
            const doc: admin.firestore.DocumentReference = admin.firestore().collection('users').doc(id);

            await doc.delete();

            resolve();
        } catch (err) {
            reject(err);
        }
    });
}
