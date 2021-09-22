import {UserSocial} from '@deckdeckgo/editor';

import {toDate} from '@deckdeckgo/editor';

import {Time} from '../../canisters/deck/deck.did';

// TODO: remove

export class CanisterUtils {
  // TODO: typescript declaration not correctly generated by SDK -> https://forum.dfinity.org/t/fail-to-verify-certificate-in-development-update-calls/4078/14
  static toNullable<T>(value?: T): [] | [T] {
    return value ? [value] : [];
  }

  static fromNullable<T>(value: [] | [T]): T | undefined {
    return value?.[0];
  }

  static toTimestamp(value: Date): Time {
    return BigInt(toDate(value).getTime());
  }

  static toNullableTimestamp(value?: Date): [] | [Time] {
    const time: number | undefined = toDate(value)?.getTime();

    return value && !isNaN(time) ? [this.toTimestamp(value)] : [];
  }

  static fromTimestamp(value: Time): Date {
    return new Date(Number(value));
  }

  static fromNullableTimestamp(value?: [] | [Time]): Date | undefined {
    return !isNaN(parseInt(`${value?.[0]}`)) ? new Date(`${value[0]}`) : undefined;
  }

  // Try to parse to number or boolean from string. It it fails, as for a string, use the value as it.
  static fromValue(value: string): any {
    try {
      return JSON.parse(value);
    } catch (err) {
      return Array.isArray(value) ? value[0] : value;
    }
  }

  static fromUserSocial<T>(userSocial: [] | [T]): UserSocial | undefined {
    const result: UserSocial = Object.keys(userSocial?.[0] || {}).reduce((acc: UserSocial, key: string) => {
      const value = CanisterUtils.fromValue(userSocial[0][key]);
      if (value) {
        acc[key] = value;
      }
      return acc;
    }, {} as UserSocial);

    return Object.keys(result).length ? result : undefined;
  }

  static toUserSocial<T>(social: UserSocial | undefined): [] | [T] {
    if (!social) {
      return [];
    }

    const {dev, linkedin, twitter, custom_logo_url, custom, github, medium} = social;

    return [
      {
        dev: CanisterUtils.toNullable<string>(dev),
        linkedin: CanisterUtils.toNullable<string>(linkedin),
        twitter: CanisterUtils.toNullable<string>(twitter),
        custom_logo_url: CanisterUtils.toNullable<string>(custom_logo_url),
        custom: CanisterUtils.toNullable<string>(custom),
        github: CanisterUtils.toNullable<string>(github),
        medium: CanisterUtils.toNullable<string>(medium)
      } as unknown as T
    ];
  }

  static async toArray<T>(data: T): Promise<Array<number>> {
    const blob: Blob = new Blob([JSON.stringify(data)], {type: 'application/json; charset=utf-8'});
    return [...new Uint8Array(await blob.arrayBuffer())];
  }

  static async fromArray<T>(data: Array<number>): Promise<T> {
    const blob: Blob = new Blob([new Uint8Array(data)], {type: 'application/json; charset=utf-8'});
    return JSON.parse(await blob.text());
  }
}
