import {ApiUnsplashProvider} from './api.unsplash.provider';

export class ApiUnsplashMockProvider extends ApiUnsplashProvider {
  // @Override
  getPhotos(_searchTerm: string, _next: string | number): Promise<UnsplashSearchResponse | undefined> {
    return new Promise<UnsplashSearchResponse>(async (resolve) => {
      const result: UnsplashSearchResponse = {
        total: 1,
        total_pages: 1,
        results: [
          {
            id: 'BDsBhMubuYM',
            description: 'mountain covered with snow under orange and blue sky at daytime',
            user: {
              name: 'Samuel Ferrara',
              links: {
                html: 'https://unsplash.com/@samferrara',
                likes: 'https://api.unsplash.com/users/samferrara/likes',
                photos: 'https://api.unsplash.com/users/samferrara/photos',
                portfolio: 'https://api.unsplash.com/users/samferrara/portfolio',
                self: 'https://api.unsplash.com/users/samfer'
              }
            },
            links: {
              download: 'https://unsplash.com/photos/BDsBhMubuYM/download',
              download_location: 'https://api.unsplash.com/photos/BDsBhMubuYM/download',
              html: 'https://unsplash.com/photos/BDsBhMubuYM',
              self: 'https://api.unsplash.com/photos/BDsBhMubuYM'
            },
            urls: {
              full: 'https://images.unsplash.com/photo-1518810765707-4f7d5d811ce0?ixlib=rb-1.2.1&q=85&fm=jpg&crop=entropy&cs=srgb&ixid=eyJhcHBfaWQiOjcyMTQyfQ',
              raw: 'https://images.unsplash.com/photo-1518810765707-4f7d5d811ce0?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjcyMTQyfQ',
              regular:
                'https://images.unsplash.com/photo-1518810765707-4f7d5d811ce0?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=1080&fit=max&ixid=eyJhcHBfaWQiOjcyMTQyfQ',
              small:
                'https://images.unsplash.com/photo-1518810765707-4f7d5d811ce0?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=400&fit=max&ixid=eyJhcHBfaWQiOjcyMTQyfQ',
              thumb:
                'https://images.unsplash.com/photo-1518810765707-4f7d5d811ce0?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=200&fit=max&ixid=eyJhcHBfaWQiOjcyMTQyfQ'
            }
          },
          {
            id: '8eQbOMgndGQ',
            description: 'Free Lugano Image',
            user: {
              name: 'Samuel Ferrara',
              links: {
                html: 'https://unsplash.com/@samferrara',
                likes: 'https://api.unsplash.com/users/samferrara/likes',
                photos: 'https://api.unsplash.com/users/samferrara/photos',
                portfolio: 'https://api.unsplash.com/users/samferrara/portfolio',
                self: 'https://api.unsplash.com/users/samfer'
              }
            },
            links: {
              download: 'https://unsplash.com/photos/8eQbOMgndGQ/download',
              download_location: 'https://api.unsplash.com/photos/8eQbOMgndGQ/download',
              html: 'https://unsplash.com/photos/8eQbOMgndGQ',
              self: 'https://api.unsplash.com/photos/8eQbOMgndGQ'
            },
            urls: {
              full: 'https://images.unsplash.com/photo-1518732439305-3830683f536c?ixlib=rb-1.2.1&q=85&fm=jpg&crop=entropy&cs=srgb&ixid=eyJhcHBfaWQiOjEyMDd9',
              raw: 'https://images.unsplash.com/photo-1518732439305-3830683f536c?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9',
              regular:
                'https://images.unsplash.com/photo-1518732439305-3830683f536c?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=1080&fit=max&ixid=eyJhcHBfaWQiOjEyMDd9',
              small:
                'https://images.unsplash.com/photo-1518732439305-3830683f536c?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=400&fit=max&ixid=eyJhcHBfaWQiOjEyMDd9',
              thumb:
                'https://images.unsplash.com/photo-1518732439305-3830683f536c?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=200&fit=max&ixid=eyJhcHBfaWQiOjEyMDd9'
            }
          }
        ]
      };

      resolve(result);
    });
  }

  // @Override
  registerDownload(_photoId: string): Promise<void> {
    return new Promise<void>(async (resolve) => {
      resolve();
    });
  }
}
