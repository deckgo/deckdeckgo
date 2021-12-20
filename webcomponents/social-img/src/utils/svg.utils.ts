export const svgToCanvas = ({svg}: {svg: SVGGraphicsElement}): Promise<HTMLCanvasElement> => {
  return new Promise<HTMLCanvasElement>((resolve) => {
    const {width, height} = svgSize(svg);

    console.log(svg.outerHTML);

    const blob: Blob = new Blob([svg.outerHTML], {type: 'image/svg+xml;charset=utf-8'});
    const blobURL: string = URL.createObjectURL(blob);

    const image = new Image();

    image.crossOrigin = 'anonymous';

    image.onload = () => {
      const canvas: HTMLCanvasElement = document.createElement('canvas');

      canvas.width = width;
      canvas.height = height;

      const context: CanvasRenderingContext2D | null = canvas.getContext('2d');
      context?.drawImage(image, 0, 0, width, height);

      URL.revokeObjectURL(blobURL);

      resolve(canvas);
    };

    image.src = blobURL;
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
