export interface UnsplashUserLinks {
  self: string;
  html: string;
  photos: string;
  likes: string;
  portfolio: string;
}

export interface UnsplashUserProfileImage {
  small: string;
  medium: string;
  large: string;
}

export interface UnsplashUser {
  id?: string;
  username?: string;
  name: string;
  portfolio_url?: string;
  bio?: string;
  location?: string;
  total_likes?: number;
  total_photos?: number;
  total_collections?: number;
  instagram_username?: string;
  twitter_username?: string;
  profile_image?: UnsplashUserProfileImage;
  links: UnsplashUserLinks;
}

export interface UserCollection {
  id: number;
  title: string;
  published_at: string | Date;
  updated_at: string | Date;
  curated: boolean;
  cover_photo: string;
  user: string;
}

export interface UnsplashPhotoUrls {
  raw: string;
  full: string;
  regular: string;
  small: string;
  thumb: string;
}

export interface UnsplashPhotoLinks {
  self: string;
  html: string;
  download: string;
  download_location: string;
}

export interface UnsplashPhoto {
  id: string;
  created_at?: string | Date;
  updated_at?: string | Date;
  width?: number;
  height?: number;
  color?: string;
  likes?: number;
  liked_by_user?: boolean;
  description?: string;

  user: UnsplashUser;
  current_user_collections?: UserCollection[];
  urls: UnsplashPhotoUrls;
  links: UnsplashPhotoLinks;
}

export interface UnsplashSearchResponse {
  total: number;
  total_pages: number;
  results: UnsplashPhoto[];
}
