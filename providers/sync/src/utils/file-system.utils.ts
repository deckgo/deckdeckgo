export const save = (data: {filename: string | undefined; blob: Blob}): Promise<void> => {
  if ('showSaveFilePicker' in window) {
    return exportNativeFileSystem(data);
  }

  return download(data);
};

const exportNativeFileSystem = async ({blob, filename}: {blob: Blob; filename: string}) => {
  const fileHandle: FileSystemFileHandle = await getNewFileHandle({filename});

  if (!fileHandle) {
    throw new Error('Cannot access filesystem');
  }

  await writeFile({fileHandle, blob});
};

const getNewFileHandle = ({filename}: {filename: string}): Promise<FileSystemFileHandle> => {
  const opts: SaveFilePickerOptions = {
    suggestedName: filename,
    types: [
      {
        description: 'DeckDeckGo Files',
        accept: {
          'application/octet-stream': ['.ddg']
        }
      }
    ]
  };

  return showSaveFilePicker(opts);
};

const writeFile = async ({fileHandle, blob}: {fileHandle: FileSystemFileHandle; blob: Blob}) => {
  const writer = await fileHandle.createWritable();
  await writer.write(blob);
  await writer.close();
};

const download = async ({filename, blob}: {filename: string; blob: Blob}) => {
  const a: HTMLAnchorElement = document.createElement('a');
  a.style.display = 'none';
  document.body.appendChild(a);

  const url: string = window.URL.createObjectURL(blob);

  a.href = url;
  a.download = `${filename}.md`;

  a.click();

  window.URL.revokeObjectURL(url);
  a.parentElement?.removeChild(a);
};
