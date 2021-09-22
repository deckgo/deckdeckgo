import {UserSocial} from '@deckdeckgo/editor';

import {toDate} from '@deckdeckgo/editor';

import {Time} from '../canisters/deck/deck.did';

// See following link for a discussion about the format of the nullable values in the did files: https://forum.dfinity.org/t/fail-to-verify-certificate-in-development-update-calls/4078/14
export const toNullable = <T>(value?: T): [] | [T] => {
  return value ? [value] : [];
};

export const fromNullable = <T>(value: [] | [T]): T | undefined => {
  return value?.[0];
};

export const toTimestamp = (value: Date): Time => {
  return BigInt(toDate(value).getTime());
};

export const toNullableTimestamp = (value?: Date): [] | [Time] => {
  const time: number | undefined = toDate(value)?.getTime();

  return value && !isNaN(time) ? [toTimestamp(value)] : [];
};

export const fromTimestamp = (value: Time): Date => {
  return new Date(Number(value));
};

export const fromNullableTimestamp = (value?: [] | [Time]): Date | undefined => {
  return !isNaN(parseInt(`${value?.[0]}`)) ? new Date(`${value[0]}`) : undefined;
};

// Try to parse to number or boolean from string. It it fails, as for a string, use the value as it.
export const fromValue = (value: string): any => {
  try {
    return JSON.parse(value);
  } catch (err) {
    return Array.isArray(value) ? value[0] : value;
  }
};

export const fromUserSocial = <T>(userSocial: [] | [T]): UserSocial | undefined => {
  const result: UserSocial = Object.keys(userSocial?.[0] || {}).reduce((acc: UserSocial, key: string) => {
    const value = fromValue(userSocial[0][key]);
    if (value) {
      acc[key] = value;
    }
    return acc;
  }, {} as UserSocial);

  return Object.keys(result).length ? result : undefined;
};

export const toUserSocial = <T>(social: UserSocial | undefined): [] | [T] => {
  if (!social) {
    return [];
  }

  const {dev, linkedin, twitter, custom_logo_url, custom, github, medium} = social;

  return [
    {
      dev: toNullable<string>(dev),
      linkedin: toNullable<string>(linkedin),
      twitter: toNullable<string>(twitter),
      custom_logo_url: toNullable<string>(custom_logo_url),
      custom: toNullable<string>(custom),
      github: toNullable<string>(github),
      medium: toNullable<string>(medium)
    } as unknown as T
  ];
};

export const toArray = async <T>(data: T): Promise<Array<number>> => {
  const blob: Blob = new Blob([JSON.stringify(data)], {type: 'application/json; charset=utf-8'});
  return [...new Uint8Array(await blob.arrayBuffer())];
};

export const fromArray = async <T>(data: Array<number>): Promise<T> => {
  const blob: Blob = new Blob([new Uint8Array(data)], {type: 'application/json; charset=utf-8'});
  return JSON.parse(await blob.text());
};
