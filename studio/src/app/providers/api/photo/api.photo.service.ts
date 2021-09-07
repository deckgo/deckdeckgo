export abstract class ApiPhotoService {
  abstract getPhotos(searchTerm: string, next: string | number): Promise<UnsplashSearchResponse | undefined>;

  abstract registerDownload(photoId: string): Promise<void>;
}
