export const svgToCanvas = ({svg}: {svg: SVGGraphicsElement}): Promise<HTMLCanvasElement> => {
  return new Promise<HTMLCanvasElement>(async (resolve) => {
    const {width, height} = svgSize(svg);

    const base64SVG: string = window.btoa(new XMLSerializer().serializeToString(svg));
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
