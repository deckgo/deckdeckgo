export const catchTab = ($event: KeyboardEvent) => {
  if ($event?.key === 'Tab') {
    $event.preventDefault();

    document.execCommand('insertHTML', false, '&#009');
  }
};
