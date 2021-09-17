import {UnsplashSearchResponse} from '../types/unsplash';

export interface GetUnsplashPhotos {
  ({apiUrl, searchTerm, next}: {apiUrl: string; searchTerm: string; next: string | number}): Promise<UnsplashSearchResponse | undefined>;
}

export interface RegisterUnsplashDownload {
  ({apiUrl, photoId}: {apiUrl: string; photoId: string}): Promise<void>;
}
