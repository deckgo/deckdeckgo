import DateTimeFormatOptions = Intl.DateTimeFormatOptions;

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

const dateOptions: DateTimeFormatOptions = {
  year: 'numeric',
  month: 'short',
  day: 'numeric',
  hour: '2-digit',
  minute: '2-digit',
  second: '2-digit',
  hour12: false
};

export const now = (): string => {
  const now: string = new Intl.DateTimeFormat('en-US', dateOptions).format(new Date());

  return now.replace(/,/g, '').replace(/:/g, '-');
};

export const formatDate = (date: Date | number | BigInt): string => {
  const parseDate: Date | undefined = toDate(date);

  if (parseDate === undefined) {
    return '';
  }

  return new Intl.DateTimeFormat('en-US', dateOptions).format(parseDate);
};
