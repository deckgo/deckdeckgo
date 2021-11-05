export const attachPasteEvent = () => document.addEventListener('paste', filterPasteText);

export const detachPasteEvent = () => document.removeEventListener('paste', filterPasteText);

const filterPasteText = ($event: ClipboardEvent) => {
  $event.preventDefault();

  const text: string = $event.clipboardData.getData('text/plain');

  document.execCommand('insertText', false, text);
};
