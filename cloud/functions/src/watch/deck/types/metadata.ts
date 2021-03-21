export interface MetadataSlide {
  background: string;
  text?: string;
}

export interface Metadata {
  slides: MetadataSlide[];
  fontFamily?: string;
}
