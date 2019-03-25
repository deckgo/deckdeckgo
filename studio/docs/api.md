# APIs

## List of endpoints

| Endpoint | Guard | Details | Frontend`*` | Backend`*` | Notes |
|---|---|---|---|---|---|
| login | X | [Link](#login) |   |   |   |
| logout | X | [Link](#logout)  |   |   |   |
| decks | X | [Link](#decks)  | POST PUT | POST PUT | Meta still TODO |
| slides | X |   |   |   |   |
| publish | X |   |   |   |   |
| feed |   |   |   |   |   |

`*`: already implemented in

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
export interface Meta {
    meta_id?: string;
    
    description?: string;
    author?: string;
    
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
