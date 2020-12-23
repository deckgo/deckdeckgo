export function getDateObj(myDate: any): Date | null {
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

  // A Firebase Timestamp format which for some reason has no mapped function
  if (myDate && (myDate.seconds >= 0 || myDate.seconds < 0) && (myDate.nanoseconds >= 0 || myDate.nanoseconds < 0)) {
    return new Date(myDate.seconds * 1000);
  }

  return myDate;
}
