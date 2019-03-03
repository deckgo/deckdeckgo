export class Comparator {

    static isEmpty(obj: any): boolean {
        return !obj || Object.keys(obj).length === 0;
    }

    static isNotEmpty(obj: any): boolean {
        return !this.isEmpty(obj);
    }

    static isStringEmpty(str: string): boolean {
        return !str || 0 === str.length;
    }

    static isStringNotEmpty(str: string): boolean {
        return !this.isStringEmpty(str);
    }

    static hasElements(obj: any[]): boolean {
        return !this.isEmpty(obj) && obj.length > 0;
    }
}

export class Converter {

    static getDateObj(myDate: any): Date {
        if (myDate == null) {
            return null;
        }

        if (myDate instanceof String || typeof myDate === 'string') {
            return new Date('' + myDate);
        }

        return myDate;
    }
}
