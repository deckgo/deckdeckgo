export const svgToCanvas = ({svg, style}: {svg: SVGGraphicsElement; style: CSSStyleDeclaration}): Promise<HTMLCanvasElement> => {
  return new Promise<HTMLCanvasElement>(async (resolve) => {
    const {width, height} = svgSize(svg);

    const clone: SVGGraphicsElement = svg.cloneNode(true) as SVGGraphicsElement;

    inlineStyle({clone, style});

    const base64SVG: string = window.btoa(new XMLSerializer().serializeToString(clone));
    const imgSrc: string = `data:image/svg+xml;base64,${base64SVG}`;

    const image = new Image();

    image.crossOrigin = 'anonymous';

    image.onload = () => {
      const canvas: HTMLCanvasElement = document.createElement('canvas');

      canvas.width = width;
      canvas.height = height;

      const context: CanvasRenderingContext2D | null = canvas.getContext('2d');
      context?.drawImage(image, 0, 0, width, height);

      resolve(canvas);
    };

    image.src = imgSrc;
  });
};

export const canvasToBlob = async ({canvas, type}: {canvas: HTMLCanvasElement; type: string}): Promise<Blob> => {
  const dataUrl: string = canvas.toDataURL(type);
  return (await fetch(dataUrl)).blob();
};

const svgSize = (svg: SVGGraphicsElement): {width: number; height: number} => {
  const width: number = svg.hasAttribute('width') ? parseInt(svg.getAttribute('width') as string, 0) : 1920;
  const height: number = svg.hasAttribute('height') ? parseInt(svg.getAttribute('height') as string, 0) : 1080;

  return {
    width,
    height
  };
};

// To print out the foreignObject with style, we have to inline the CSS that has been set
const inlineStyle = ({clone, style}: {clone: SVGGraphicsElement; style: CSSStyleDeclaration}) => {
  const text: HTMLParagraphElement | null = clone.querySelector('foreignObject > p');

  if (!text) {
    return;
  }

  for (const key of Object.keys(style)) {
    text.style.setProperty(key, style[key]);
  }
};
