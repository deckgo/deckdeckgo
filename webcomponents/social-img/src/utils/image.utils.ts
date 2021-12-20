export const fetchImage = async ({imgSrc}: {imgSrc: string}): Promise<string | undefined> => {
  const data: Response = await fetch(imgSrc);
  const blob: Blob = await data.blob();

  const base64: string = await toBase64({blob});

  return base64.split(',')?.[1];
};

const toBase64 = ({blob}: {blob: Blob}): Promise<string> => {
  return new Promise<string>((resolve, reject) => {
    try {
      const reader: FileReader = new FileReader();
      reader.onloadend = () => {
        const {result} = reader;
        resolve(result as string);
      };

      reader.readAsDataURL(blob);
    } catch (err) {
      reject(err);
    }
  });
};
