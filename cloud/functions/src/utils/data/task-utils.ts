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

export function successfulTask(taskId: string): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      await updateStatus(taskId, 'successful');

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}

export function failureTask(taskId: string): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      await updateStatus(taskId, 'failure');

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}

function updateStatus(taskId: string, status: 'scheduled' | 'failure' | 'successful'): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      if (!taskId || taskId === undefined || !taskId) {
        resolve();
        return;
      }

      const documentReference: admin.firestore.DocumentReference = admin.firestore().doc(`/tasks/${taskId}/`);

      const updateTaskData: Partial<TaskData> = {
        status: status,
        token: admin.firestore.FieldValue.delete(),
        updated_at: admin.firestore.Timestamp.now(),
      };

      await documentReference.set(updateTaskData, {merge: true});

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}
