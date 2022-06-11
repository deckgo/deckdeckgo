import { get } from 'idb-keyval';

// Fetch asset regardless if save in indexedDB or online
export const fetchAsset = (src: string): Promise<string> => {
  if (src.indexOf('http') === -1) {
    return fetchLocalData(src);
  }

  return fetchData(src);
}

const fetchData = async (src: string): Promise<string> => {
  const rawResponse: Response = await fetch(src);
  return rawResponse.text();
}

const fetchLocalData = async (src: string): Promise<string> => {
  const savedData: File = await get(src);

  const URL = window.URL || window.webkitURL;
  const url: string = URL.createObjectURL(savedData);

  const content: string = await fetchData(url);

  URL.revokeObjectURL(url);

  return content;
}
