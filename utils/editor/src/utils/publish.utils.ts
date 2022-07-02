import {Deck} from '../models/data/deck';
import {Doc} from '../models/data/doc';
import {Meta} from '../models/data/meta';
import {deckSelector} from './deck.utils';
import {docSelector} from './doc.utils';
import {getGoogleFontUrl, GoogleFont, googleFonts} from './fonts.utils';
import {cleanNode, dirtyPublishAttributes, isElementNode} from './node.utils';

export interface PublishData {
  title: string;
  description: string;
  author: string;
  bio: string | undefined;
  photo_url: string | undefined;
  head_extra: string | undefined;
  attributes: Record<string, string> | undefined;
  social_image_name: string;
  social_image_value: Blob | undefined;
  social_links: string | undefined;
  social_image_link: string | undefined;
}

export interface DeckPublishData extends PublishData {
  slides: string[];
  background: string | undefined;
  header: string | undefined;
  footer: string | undefined;
}

export interface DocPublishData extends PublishData {
  paragraphs: string[];
  theme: string | undefined;
}

export const deckPublishData = async ({deck, fallbackAuthor}: {deck: Deck; fallbackAuthor: string}): Promise<DeckPublishData> => {
  const {data} = deck;
  const {meta, background, footer, header} = data;

  return {
    ...(await publishData({meta, selector: deckSelector, fallbackName: data.name, fallbackAuthor})),
    slides: getSlides(),
    background: background ? `<div slot="background">${background}</div>` : undefined,
    header: background ? `<div slot="header">${header}</div>` : undefined,
    footer: background ? `<div slot="footer">${footer}</div>` : undefined
  };
};

export const docPublishData = async ({
  doc,
  fallbackAuthor,
  theme,
  socialImgPath
}: {
  doc: Doc;
  fallbackAuthor: string;
  theme: string | undefined;
  socialImgPath?: string;
}): Promise<DocPublishData> => {
  const {data} = doc;
  const {meta} = data;

  return {
    ...(await publishData({meta, selector: docSelector, fallbackName: data.name, fallbackAuthor, socialImgPath})),
    paragraphs: getParagraphs(),
    theme,
    social_image_link: getDocSocialImgLink({selector: docSelector})
  };
};

const publishData = async ({
  meta,
  fallbackName,
  fallbackAuthor,
  selector,
  socialImgPath
}: {
  meta: Meta | undefined;
  fallbackName: string;
  fallbackAuthor: string;
  selector: string;
  socialImgPath?: string;
}): Promise<PublishData> => {
  const googleFontLink: string | undefined = getGoogleFontLink();
  const canonicalLink: string | undefined = getCanonicalLink({meta});

  const head_extra: string[] = [googleFontLink, canonicalLink].filter((link: string | undefined) => link !== undefined) as string[];

  const attributes: Record<string, string> | undefined = getAttributes({selector});

  const socialImage: Blob | undefined = await getSocialImage();

  const title: string = (meta?.title || fallbackName)?.trim();

  return {
    title,
    description: (meta?.description || fallbackName)?.trim(),
    author: meta?.author?.name || fallbackAuthor,
    bio: meta?.author?.bio,
    photo_url: meta?.author?.photo_url,
    head_extra: head_extra.length > 0 ? head_extra.join('') : undefined,
    attributes,
    social_image_name: encodeURI(title).toLowerCase(),
    social_image_value: socialImage,
    social_links: buildSocialLinks({meta, socialImgPath}),
    social_image_link: undefined
  };
};

const getGoogleFontLink = (): string | undefined => {
  const deck: HTMLElement | null = document.querySelector(deckSelector);
  const fontFamily: string | undefined = deck?.style.fontFamily;

  if (!fontFamily) {
    return undefined;
  }

  const font: GoogleFont | undefined = googleFonts.find((filteredFont: GoogleFont) => {
    return fontFamily === filteredFont.family.replace(/\'/g, '');
  });

  if (!font) {
    return undefined;
  }

  const url: string = getGoogleFontUrl({font});

  return `<link rel="stylesheet" href="${url}">`;
};

const getCanonicalLink = ({meta}: {meta: Meta | undefined}): string | undefined => {
  if (!meta || !meta.canonical) {
    return undefined;
  }

  return `<link rel="canonical" href="${meta.canonical}">`;
};

const getAttributes = ({selector}: {selector: string}): Record<string, string> | undefined => {
  const element: HTMLElement | null = document.querySelector(selector);
  const attr: NamedNodeMap | undefined = element?.attributes;

  if (!attr) {
    return undefined;
  }

  return Array.from(attr)
    .filter(({name}: Attr) => !['id', 'hydrated', 'class', 'contenteditable'].includes(name))
    .reduce((acc, {name, value}: Attr) => {
      acc[name] = value;
      return acc;
    }, {} as Record<string, string>);
};

const getSlides = (): string[] => {
  const slides: NodeListOf<HTMLElement> = document.querySelectorAll(`${deckSelector} > *[slide_id]`);

  const removeCustomSlot = ({slide, custom}: {slide: HTMLElement; custom: 'background' | 'header' | 'footer'}) => {
    if (slide.hasAttribute(`custom-${custom}`)) {
      return;
    }

    const slot: HTMLElement | null = slide.querySelector(`div[slot="${custom}"]`);

    if (!slot) {
      return;
    }

    slide.removeChild(slot);
  };

  const cloneSlides: HTMLElement[] = Array.from(slides).map((slide: HTMLElement) => {
    const cloneSlide: HTMLElement = cleanNode({node: slide, deep: false, attributes: dirtyPublishAttributes}) as HTMLElement;

    const nodes: Node[] = Array.from(slide.childNodes);
    nodes.forEach((node: Node) => {
      if (isElementNode(node) && (node as HTMLElement).hasAttribute('slot')) {
        cloneSlide.appendChild(cleanNode({node, attributes: dirtyPublishAttributes}) as Node);

        removeCustomSlot({slide: cloneSlide, custom: 'background'});
        removeCustomSlot({slide: cloneSlide, custom: 'header'});
        removeCustomSlot({slide: cloneSlide, custom: 'footer'});
      }
    });

    return cloneSlide;
  });

  return cloneSlides.map((clone: HTMLElement) => clone.outerHTML);
};

const getParagraphs = (): string[] => {
  const paragraphs: NodeListOf<HTMLElement> = document.querySelectorAll(`${docSelector} > article *[paragraph_id]`);

  const cloneParagraphs: HTMLElement[] = Array.from(paragraphs).map(
    (paragraph: HTMLElement) => cleanNode({node: paragraph, attributes: dirtyPublishAttributes}) as HTMLElement
  );

  return cloneParagraphs.map((clone: HTMLElement) => clone.outerHTML);
};

const getSocialImage = async (): Promise<Blob | undefined> => {
  const deckGoSocialImg: HTMLDeckgoSocialImgElement | null = document.querySelector('deckgo-social-img');
  return deckGoSocialImg?.toBlob('image/png');
};

const buildSocialLinks = ({meta, socialImgPath}: {meta: Meta | undefined; socialImgPath?: string}): string | undefined => {
  if (!meta || !meta.author) {
    return undefined;
  }

  const {
    author: {social, name}
  }: Meta = meta;

  const buildSocialLink = ({
    username,
    href,
    authorName,
    platformName,
    iconName
  }: {
    username: string | undefined;
    href: string;
    authorName: string;
    platformName: string;
    iconName: string;
  }): string | undefined =>
    !username || username === ''
      ? undefined
      : `<a href="https://${href}/${username}" aria-label="${authorName} on ${platformName}" rel="noopener norefferer"><deckgo-lazy-img svg-src="${socialImgPath}/${iconName}.svg" role="presentation" alt="${platformName} logo" /></a>`;

  const buildCustomLink = ({custom, authorName}: {custom: string | undefined; authorName: string}): string | undefined =>
    !custom || custom === ''
      ? undefined
      : `<a href="${custom}" aria-label="${authorName}" rel="noopener norefferer"><deckgo-lazy-img svg-src="${socialImgPath}/globe.svg" role="presentation" alt="" /></a>`;

  const twitterLink: string | undefined = buildSocialLink({
    username: social?.twitter,
    href: 'twitter.com',
    authorName: name,
    platformName: 'Twitter',
    iconName: 'twitter'
  });

  const linkedInLink: string | undefined = buildSocialLink({
    username: social?.linkedin,
    href: 'www.linkedin.com/in',
    authorName: name,
    platformName: 'LinkedIn',
    iconName: 'linkedin'
  });

  const githubLink: string | undefined = buildSocialLink({
    username: social?.github,
    href: 'github.com',
    authorName: name,
    platformName: 'GitHub',
    iconName: 'github'
  });

  const customUrl: string | undefined = buildCustomLink({
    custom: social?.custom,
    authorName: name
  });

  const hasLinks: boolean = twitterLink !== undefined || linkedInLink !== undefined || githubLink !== undefined || customUrl !== undefined;

  return hasLinks
    ? `${[customUrl, twitterLink, githubLink, linkedInLink].filter((link: string | undefined) => link !== undefined).join('')}`
    : undefined;
};

const getDocSocialImgLink = ({selector}: {selector: string}): string | undefined => {
  const firstParagraph: HTMLElement | null = document.querySelector(`${selector} > article *:nth-child(1)`);
  const secondParagraph: HTMLElement | null = document.querySelector(`${selector} > article *:nth-child(2)`);

  const getImgSrc = (element: HTMLElement | null | undefined): string | undefined => {
    const attribute: string | null | undefined = element?.getAttribute('img-src');
    return attribute?.indexOf('http') === 0 ? attribute : undefined;
  }

  if (firstParagraph?.nodeName.toLowerCase() === 'deckgo-lazy-img') {
    return getImgSrc(firstParagraph);
  }

  if (secondParagraph?.nodeName.toLowerCase() === 'deckgo-lazy-img') {
    return getImgSrc(secondParagraph);
  }

  const firstParagraphChild: HTMLElement | null | undefined = firstParagraph?.querySelector('deckgo-lazy-img');
  if (firstParagraphChild !== null && firstParagraphChild !== undefined) {
    return getImgSrc(firstParagraphChild);
  }

  const secondParagraphChild: HTMLElement | null | undefined = secondParagraph?.querySelector('deckgo-lazy-img');
  return getImgSrc(secondParagraphChild);
};
