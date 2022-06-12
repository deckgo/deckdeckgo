import Excalidraw from '@excalidraw/excalidraw';
import type {NonDeletedExcalidrawElement} from '@excalidraw/excalidraw/types/element/types';
import type {AppState} from '@excalidraw/excalidraw/types/types';
import {createElement, useRef} from 'react';
import {render} from 'react-dom';

let excalidrawRef;

export interface ExcalidrawScene {
  elements: NonDeletedExcalidrawElement[];
  appState: AppState;
}

const mergeScene = (scene: ExcalidrawScene | undefined): ExcalidrawScene => ({
  elements: [],
  appState: {currentItemFontFamily: 1},
  ...(scene && scene)
});

const App = (scene) => {
  excalidrawRef = useRef(null);

  const initialData = mergeScene(scene);

  return createElement(
    'div',
    {
      className: 'excalidraw-wrapper'
    },
    createElement(Excalidraw.default, {
      initialData,
      ref: excalidrawRef,
      UIOptions: {
        canvasActions: {
          saveToActiveFile: false,
          theme: false,
          saveAsImage: false
        }
      }
    })
  );
};

export const renderExcalidraw = ({host, scene}) => {
  render(createElement(App, scene), host);
};

const assertExcalidrawRef = () => {
  if (!excalidrawRef || excalidrawRef === undefined) {
    throw new Error('No reference to Excalidraw defined. Ist the React app mounted?')
  }
}

export const exportToBlob = (mimeType: string = 'image/webp'): Promise<Blob> => {
  assertExcalidrawRef();

  const excalidrawDiagram = {
    type: 'excalidraw',
    version: 2,
    source: 'https://excalidraw.com',
    elements: excalidrawRef.current.getSceneElements(),
    appState: excalidrawRef.current.getAppState()
  };

  return Excalidraw.exportToBlob({...excalidrawDiagram, mimeType});
};

export const getScene = (): ExcalidrawScene => {
  assertExcalidrawRef();

  return {
    elements: excalidrawRef.current.getSceneElements(),
    appState: excalidrawRef.current.getAppState()
  };
};

export const updateScene = (scene: ExcalidrawScene | undefined) => {
  assertExcalidrawRef();

  const data = mergeScene(scene);

  excalidrawRef.current.updateScene(data);
};
