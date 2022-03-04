export interface TenorMediaObject {
  preview: string;
  url: string;
  dims: number[];
  size: number;
}

export interface TenorMedia {
  gif: TenorMediaObject;
  mediumgif: TenorMediaObject;
  tinygif: TenorMediaObject;
  nanogif: TenorMediaObject;
  mp4: TenorMediaObject;
  loopedmp4: TenorMediaObject;
  tinymp4: TenorMediaObject;
  nanomp4: TenorMediaObject;
  webm: TenorMediaObject;
  tinywebm: TenorMediaObject;
  nanowebm: TenorMediaObject;
}

export interface TenorGif {
  id: string;

  created: number;

  hasaudio: boolean;
  shares: number;

  title: string;
  tags: string[];

  url: string;
  itemurl: string;

  media: TenorMedia[];
}

export interface TenorSearchResponse {
  results: TenorGif[];
  next: string | number;
}

export interface TenorCategory {
  searchterm: string;
  path: string;
  image: string;
  name: string;
}

export interface TenorCategoryResponse {
  tags: TenorCategory[];
}

export interface TenorAnonymousResponse {
  anon_id: string;
}

export interface TenorRegisterShareResponse {
  status: string;
}
