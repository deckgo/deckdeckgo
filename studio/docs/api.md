# APIs

## List of endpoints

| Endpoint | Guard | Details | Frontend`*` | Backend`*` | Notes |
|---|---|---|---|---|---|
| login |  | [Link](#login) |   |   |   |
| logout | X | [Link](#logout)  |   |   |   |
| decks | X | [Link](#decks) | POST PUT | POST PUT | Meta and guard still TODO |
| slides | X | [Link](#slides) | POST PUT DELETE | POST | Furthermore than guard and missing routes, content should be saved outside the db |
| tags | X | [Link](#tags) |   |   |   |
| content | X | [Link](#content) |   |   | Images and other medias |
| publish | X | [Link](#publish) |   |   |   |
| feed |  | [Link](#feed) |   |   |   |

`*`: already implemented in

## Others TODO

- replace CORS wildcard with proper `Access-Control-Allow-Origin`

### Login

| <!-- -->    | <!-- -->    |
|-------------|-------------|
| Title | Login |
| Description | Verify Google ID Token, create or update user, convert anonymous data to "real" user |
| URL | /login |
| Reference | [https://firebase.google.com/docs/auth/admin/verify-id-tokens](https://firebase.google.com/docs/auth/admin/verify-id-tokens) |
| Method | POST |
| Body | token (?) |
| Success Response | `{backend_token: string}` |

* User data sample:

```
export interface User {
    token: string;
    
    backend_token: string;

    anonymous: boolean;

    name?: string;
    email?: string;
    email_verified?: boolean;
    photo_url?: string;
    
    created_at: Date;
    updated_at: Date;
}
```

* Questions:

- in the definition add `backend_user_id`?

* Notes

- If *verify-id-tokens* from Google doesn't return user data on the backend side, user data has to be provided as body of the request

- `backend_token` is the token to be provided in each guarded backend routes

### Logout

| <!-- -->    | <!-- -->    |
|-------------|-------------|
| Title | Logout |
| Description | Invalidate backend token |
| URL | /logout |
| Method | POST |
| Body | token (?) |

* Notes

- should we also add a status on the user data and therefore set the status to `inactive` or else on logout?

### Decks

| <!-- -->    | <!-- -->    |
|-------------|-------------|
| Title | Decks |
| Description | The decks, the presentation entities |
| URL | /decks |
| Method | POST PUT DELETE |
| Body | see sample |

* Sample

```
export interface MetaTag {
    string;
}

export interface Meta {
    meta_id?: string;
    
    description?: string;
    author?: string;
    
    tags: MetaTag[];
    
    image_url?: string;
    
    created_at: Date;
    updated_at: Date;

}

export interface Deck {
    deck_id?: string;
    deck_slides: string[];
    
    meta: Meta;
    
    created_at: Date;
    updated_at: Date;
}
```

Questions:

- do we want to duplicate the author name (= user name) in the meta data? for performance reason maybe?

Notes:

- basically the corresponding DeckDeckGo core Web Component is `<deckgo-deck/>`

### Slides

| <!-- -->    | <!-- -->    |
|-------------|-------------|
| Title | Slides |
| Description | The slides of the presentation |
| URL | /slides |
| Method | POST PUT DELETE |
| Body | see sample |

* Sample

```
export enum SlideTemplate {
    TITLE = 'title',
    CONTENT = 'content',
    SPLIT = 'split'
}

export interface SlideAttributes {
    style?: string;
}

export interface Deck {
    slide_id?: string;
    
    slide_content?: string;
    slide_template: SlideTemplate,
    slide_attributes?: SlideAttributes;
    
    created_at: Date;
    updated_at: Date;
}
```

Notes:

- basically the corresponding DeckDeckGo core Web Components are all `<deckgo-slide-xxxxxx/>` and their content
- `SlideTemplate` and `SlideAttributes` will be extended in the future

### Tags

| <!-- -->    | <!-- -->    |
|-------------|-------------|
| Title | Tags |
| Description | Return a list of matching meta tags in regards of the searchterm |
| URL | /feed |
| Method | GET |
| Body | searchTerm |

Notes:

- filter and search case not sensitive and stuffs
- the goal is to have only on time if possible for example "javascript" in the database

### Content

| <!-- -->    | <!-- -->    |
|-------------|-------------|
| Title | Content |
| Description | To upload content to th s3, like images, we will need signedUrl. These will have to be generated from the backend. |
| URL | /content |
| Method | POST |
| Body | fileName??? |

### Publish

| <!-- -->    | <!-- -->    |
|-------------|-------------|
| Title | Publish |
| Description | Publish presentation should build the PWA presentation and unleash it on the web |
| URL | /publish |
| Method | POST |
| Body | deck_id (?) |

### Feed

| <!-- -->    | <!-- -->    |
|-------------|-------------|
| Title | Feed |
| Description | The feed returns decks and meta |
| URL | /feed |
| Method | GET |
| Body | ??? |

Notabene:

- Right now with the current design we only need the content (deck and slides) of the first deck of the feed. The others are only displayed with their meta information. Maybe that could be an option of the route? with/without content? 

Notes:

- At some point the feed will be optimized and specific pro users regarding his/her interested
- Need pagination. The feed should for example be fetched for 10 presentations and then 100 pro 100
