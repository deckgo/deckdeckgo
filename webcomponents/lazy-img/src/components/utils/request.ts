// From Ionicons
// https://github.com/ionic-team/ionicons/blob/master/src/components/icon/request.ts

import {validateContent} from './validate';

const requests = new Map<string, Promise<string>>();

export const getSvgContent = (url: string) => {
  // see if we already have a request for this url
  let req = requests.get(url);

  if (!req) {
    // we don't already have a request
    req = fetch(url).then(rsp => {
      if (rsp.status <= 299) {
        return rsp.text();
      }
      return Promise.resolve(null);

    }).then(svgContent => validateContent(svgContent));

    // cache for the same requests
    requests.set(url, req);
  }

  return req;
};
