import {GetUnsplashPhotos, UnsplashSearchResponse, RegisterUnsplashDownload} from '@deckdeckgo/editor';

export const getUnsplashPhotos: GetUnsplashPhotos = ({
  apiUrl,
  searchTerm,
  next
}: {
  apiUrl: string;
  searchTerm: string;
  next: string | number;
}): Promise<UnsplashSearchResponse | undefined> => {
  return new Promise<UnsplashSearchResponse | undefined>(async (resolve, reject) => {
    if (!apiUrl) {
      resolve(undefined);
      return;
    }

    const searchUrl: string = apiUrl + 'search/photos/?query=' + searchTerm + '&page=' + next;

    try {
      const rawResponse: Response = await fetch(searchUrl);

      const response: UnsplashSearchResponse = JSON.parse(await rawResponse.text());

      if (!response) {
        resolve(undefined);
        return;
      }

      resolve(response);
    } catch (err) {
      reject(err);
    }
  });
};

export const registerUnsplashDownload: RegisterUnsplashDownload = ({apiUrl, photoId}: {apiUrl: string; photoId: string}): Promise<void> => {
  return new Promise<void>(async (resolve) => {
    if (!apiUrl) {
      resolve();
      return;
    }

    const shareUrl: string = apiUrl + 'photos/' + photoId + '/download/';

    try {
      await fetch(shareUrl);

      // We don't check the status of the answer, user could still use the photo even if that would have failed
      resolve();
    } catch (err) {
      // We ignore the error, user could still use the photo
      resolve();
    }
  });
};
