export class UserUtils {
    
    static validUsername(username: string): boolean {
        return username && username !== null && username !== undefined && username.length >= 3 && username.length <= 32;
    }

    static validName(name: string): boolean {
        return name && name !== null && name !== undefined && name.length >= 3 && name.length <= 64;
    }
}
