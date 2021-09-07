export abstract class ApiUnsplashProvider {
  abstract getPhotos(searchTerm: string, next: string | number): Promise<UnsplashSearchResponse | undefined>;

  abstract registerDownload(photoId: string): Promise<void>;
}
