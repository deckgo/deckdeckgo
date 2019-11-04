export class ParseContentUtils {

    static cleanContent(content: string): Promise<string> {
        return new Promise<string>(async (resolve) => {
            if (!content || content.length <= 0) {
                resolve(content);
                return;
            }

            let result: string = content.replace(/contenteditable=""|contenteditable="true"|contenteditable="false"|contenteditable/gi, '');
            result = result.replace(/editable=""|editable="true"|editable/gi, '');
            result = result.replace(/highlighted=""|highlighted="true"|highlighted/gi, '');
            result = result.replace(/class="[a-zA-Z0-9:;\.\s\(\)\-\,]*"/gi, '');

            resolve(result);
        });
    }

}
