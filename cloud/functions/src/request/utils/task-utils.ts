import * as admin from 'firebase-admin';

import {TaskData} from '../../model/data/task';

export function scheduleTask(data: Partial<TaskData>): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const now: admin.firestore.Timestamp = admin.firestore.Timestamp.now();

      const scheduledData: TaskData = {
        ...(data as TaskData),
        status: 'scheduled',
        created_at: now,
        updated_at: now,
      };

      const collectionRef: admin.firestore.CollectionReference = admin.firestore().collection('/tasks/');

      await collectionRef.add(scheduledData);

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}
