export class UserUtils {
    
    static validUsername(username: string): boolean {
        return username && username !== null && username !== undefined && username.length >= 3 && username.length <= 32;
    }
    
}
