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


interface TenorTrendingResponse {
    results: TenorGif[];
}

interface TenorAnonymousResponse {
    anon_id: string;
}
