import {SlideAttributes} from '../../models/data/slide';
import {DeckAttributes} from '../../models/data/deck';
import {UserSocial} from '../../models/data/user';

import {Attribute, Time} from '../../canisters/slides/slides.did';

import {toDate} from '../core/date.utils';

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

  static toAttributes(attributes: SlideAttributes | DeckAttributes): [Attribute[]] | [] {
    if (!attributes) {
      return [];
    }

    const keys: string[] = Object.keys(attributes);

    if (!keys || keys.length <= 0) {
      return [];
    }

    const results: Attribute[] = keys
      .filter((key: string) => attributes[key] !== null && attributes[key] !== undefined && attributes[key] !== '')
      .map((key: string) => ({
        name: key,
        value: `${attributes[key]}`
      }));

    return results?.length > 0 ? [results] : [];
  }

  static fromAttributes<T>(attributes: [] | [Attribute[]]): T | undefined {
    if (!attributes) {
      return undefined;
    }

    return attributes?.[0]?.reduce((acc: T, {name, value}: Attribute) => {
      try {
        acc[name] = this.fromValue(value);
      } catch (err) {
        acc[name] = value;
      }
      return acc;
    }, {} as T);
  }

  // Try to parse to number or boolean from string. It it fails, as for a string, use the value as it.
  static fromValue(value: string): any {
    try {
      return JSON.parse(value);
    } catch (err) {
      return value;
    }
  }

  static fromUserSocial<T>(userSocial: [] | [T]): UserSocial {
    return Object.keys(userSocial?.[0] || {}).reduce((acc: UserSocial, key: string) => {
      acc[key] = CanisterUtils.fromValue(userSocial[0][key]);
      return acc;
    }, {} as UserSocial);
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
}
