export class ParseStyleUtils {

    static convertStyle(originalStyle: string): Promise<any> {
        return new Promise<any>((resolve) => {
            if (!originalStyle || originalStyle.length <= 0) {
                resolve(undefined);
                return;
            }

            const result: any = {};

            const styles: string[] = originalStyle.split(';');

            if (styles && styles.length > 0) {
                styles.forEach((style: string) => {
                    if (style && style.length > 0) {
                        const split: string[] = style.split(':');
                        if (split && split.length > 1) {
                            result[split[0].trim()] = split[1].trim();
                        } else if (split && split.length > 0) {
                            result[split[0].trim()] = undefined;
                        }
                    }
                });
            }

            resolve(result);
        });
    }

}
