import {DeckData} from '../../../model/deck';

export function isDeckPublished(previousValue: DeckData, newValue: DeckData): Promise<boolean> {
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
      const previousPublishedAt: Date | null = getDateObj(previousValue.meta.updated_at);
      const newPublishedAt: Date | null = getDateObj(newValue.meta.updated_at);

      if (previousPublishedAt && newPublishedAt && newValue.meta.published) {
        updated = previousPublishedAt.getTime() < newPublishedAt.getTime();
      }
    }

    resolve(firstTimePublished || updated);
  });
}

function getDateObj(myDate: any): Date | null {
  if (myDate === null) {
    return null;
  }

  if (myDate instanceof String || typeof myDate === 'string') {
    return new Date('' + myDate);
  }

  // A Firebase Timestamp format
  if (myDate && typeof myDate.toDate === 'function' && (myDate.seconds >= 0 || myDate.seconds < 0) && (myDate.nanoseconds >= 0 || myDate.nanoseconds < 0)) {
    return new Date(myDate.toDate());
  }

  return myDate;
}
