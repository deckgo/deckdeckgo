interface TenorMediaObject {
  preview: string;
  url: string;
  dims: number[];
  size: number;
}

interface TenorMedia {
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

interface TenorGif {
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

interface TenorSearchResponse {
  results: TenorGif[];
  next: string | number;
}

interface TenorCategory {
  searchterm: string;
  path: string;
  image: string;
  name: string;
}

interface TenorCategoryResponse {
  tags: TenorCategory[];
}

interface TenorAnonymousResponse {
  anon_id: string;
}

interface TenorRegisterShareResponse {
  status: string;
}
