import {SlideAttributes} from '../../models/data/slide';
import {DeckAttributes} from '../../models/data/deck';

import {Attribute, Time} from '../../functions/slides/slides.did';

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
    return new Date(`${value[0]}`);
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
}
