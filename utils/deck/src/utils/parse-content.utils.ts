export function cleanContent(content: string): Promise<string> {
  return new Promise<string>(async (resolve) => {
    if (!content || content.length <= 0) {
      resolve(content);
      return;
    }

    let result: string = content.replace(
      /(<.*?)(contenteditable=""|contenteditable="true"|contenteditable="false"|contenteditable)(.*?>)/gi,
      '$1$3'
    );
    result = result.replace(/(<.*?)(editable=""|editable="true"|editable="false"|editable)(.*?>)/gi, '$1$3');
    result = result.replace(/(<.*?)(spellcheck=""|spellcheck="true"|spellcheck="false"|spellcheck)(.*?>)/gi, '$1$3');
    result = result.replace(/(<.*?)(highlighted=""|highlighted="true"|highlighted)(.*?>)/gi, '$1$3');
    result = result.replace(/(<.*?)(custom-loader=""|custom-loader="true"|custom-loader)(.*?>)/gi, '$1$3');
    result = result.replace(/class="[a-zA-Z0-9:;\.\s\(\)\-\,]*"/gi, '');

    resolve(result);
  });
}
