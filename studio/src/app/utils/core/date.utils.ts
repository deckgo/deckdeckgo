export const toDate = (myDate: any): Date | undefined => {
  if (!myDate || myDate === undefined) {
    return undefined;
  }

  if (myDate instanceof String || typeof myDate === 'string') {
    return new Date(`${myDate}`);
  }

  if (typeof myDate === 'number' && !isNaN(myDate)) {
    return new Date(myDate);
  }

  // A Firebase Timestamp format
  if (myDate && myDate.seconds >= 0 && myDate.nanoseconds >= 0) {
    return new Date(myDate.toDate());
  }

  return myDate;
};
