import {Excalidraw, exportToBlob as excalidrawExportToBlob} from '@excalidraw/excalidraw';
import type {NonDeletedExcalidrawElement} from '@excalidraw/excalidraw/types/element/types';
import type {AppState, BinaryFiles} from '@excalidraw/excalidraw/types/types';
import {createElement, useRef} from 'react';
import {render} from 'react-dom';

let excalidrawRef;

export interface ExcalidrawScene {
  type: 'excalidraw';
  version: number;
  source: string;
  elements: NonDeletedExcalidrawElement[];
  appState: AppState;
  files: BinaryFiles;
}

const {origin} = window.location;

const SCENE_META: Pick<ExcalidrawScene, 'type' | 'version' | 'source'> = {
  type: 'excalidraw',
  version: 2,
  source: origin
};

const mergeScene = (scene: ExcalidrawScene | undefined): ExcalidrawScene => ({
  ...SCENE_META,
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
    createElement(Excalidraw, {
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
    throw new Error('No reference to Excalidraw defined. Ist the React app mounted?');
  }
};

export const exportToBlob = (mimeType: string = 'image/webp'): Promise<Blob> => {
  assertExcalidrawRef();

  const excalidrawDiagram = {
    ...SCENE_META,
    elements: excalidrawRef.current.getSceneElements(),
    appState: excalidrawRef.current.getAppState(),
    files: excalidrawRef.current.getFiles()
  };

  return excalidrawExportToBlob({...excalidrawDiagram, mimeType});
};

export const getScene = (): ExcalidrawScene => {
  assertExcalidrawRef();

  return {
    ...SCENE_META,
    elements: excalidrawRef.current.getSceneElements(),
    appState: excalidrawRef.current.getAppState(),
    files: excalidrawRef.current.getFiles()
  };
};

export const updateScene = (scene: ExcalidrawScene | undefined) => {
  assertExcalidrawRef();

  const data = mergeScene(scene);

  excalidrawRef.current.updateScene(data);
};
