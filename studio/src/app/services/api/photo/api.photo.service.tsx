import {ErrorService} from '../../core/error/error.service';

export abstract class ApiPhotoService {
  protected errorService: ErrorService;

  public constructor() {
    // Private constructor, singleton
    this.errorService = ErrorService.getInstance();
  }

  abstract getPhotos(searchTerm: string, next: string | number): Promise<UnsplashSearchResponse>;

  abstract registerDownload(photoId: string): Promise<void>;
}
