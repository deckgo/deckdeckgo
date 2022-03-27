const elementsNotEditable: string[] = ['figure'];
const elementsEditable: string[] = ['figurecaption'];

export const contentNotEditableParagraph = (nodeName: string): {contenteditable?: 'true' | 'false'} =>
  elementsNotEditable.includes(nodeName.toLowerCase()) ? {contenteditable: 'false'} : {};

export const contentEditableParagraphChildren = ({nodeName}: HTMLElement): {contenteditable?: 'true' | 'false'} =>
  elementsEditable.includes(nodeName.toLowerCase()) ? {contenteditable: 'true'} : {};
