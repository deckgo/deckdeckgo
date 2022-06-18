export const save = (data: {filename: string | undefined; blob: Blob; types: FilePickerAcceptType[]; extension: string}): Promise<void> => {
  if ('showSaveFilePicker' in window) {
    return exportNativeFileSystem(data);
  }

  return download(data);
};

const exportNativeFileSystem = async ({blob, filename, types}: {blob: Blob; filename: string; types: FilePickerAcceptType[]}) => {
  const fileHandle: FileSystemFileHandle = await getNewFileHandle({filename, types});

  if (!fileHandle) {
    throw new Error('Cannot access filesystem');
  }

  await writeFile({fileHandle, blob});
};

const getNewFileHandle = ({filename, types}: {filename: string; types: FilePickerAcceptType[]}): Promise<FileSystemFileHandle> => {
  const opts: SaveFilePickerOptions = {
    suggestedName: filename,
    types
  };

  return showSaveFilePicker(opts);
};

const writeFile = async ({fileHandle, blob}: {fileHandle: FileSystemFileHandle; blob: Blob}) => {
  const writer = await fileHandle.createWritable();
  await writer.write(blob);
  await writer.close();
};

const download = async ({filename, blob, extension}: {filename: string; blob: Blob; extension: string}) => {
  const a: HTMLAnchorElement = document.createElement('a');
  a.style.display = 'none';
  document.body.appendChild(a);

  const url: string = window.URL.createObjectURL(blob);

  a.href = url;
  a.download = `${filename}.${extension}`;

  a.click();

  window.URL.revokeObjectURL(url);
  a.parentElement?.removeChild(a);
};
