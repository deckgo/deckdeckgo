export class UserUtils {
  static validUsername(username: string): boolean {
    return username && username !== null && username !== undefined && username.length >= 3 && username.length <= 32;
  }

  static validName(name: string): boolean {
    return name && name !== null && name !== undefined && name.length >= 3 && name.length <= 64;
  }

  static validEmail(email: string): boolean {
    const pattern: RegExp = new RegExp(
      /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/
    );
    return email && email !== null && email !== undefined && email.length >= 3 && email.length <= 254 && pattern.test(email);
  }
}
