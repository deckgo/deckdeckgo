# APIs

## List of endpoints

| Endpoint | Guard | Details | Frontend`*` | Backend`*` |
|---|---|---|---|---|
| login | X | [Link](#login) |   |   |
| logout | X | [Link](#logout)  |   |   |
| decks | X |   |   |   |
| slides | X |   |   |   |
| publish | X |   |   |   |
| feed |   |   |   |   |

`*`: already implemented in

### Login

| <!-- -->    | <!-- -->    |
|-------------|-------------|
| Title | Login |
| Description | Verify Google ID Token, create or update user, convert anonymous data to "real" user |
| Reference | [https://firebase.google.com/docs/auth/admin/verify-id-tokens](https://firebase.google.com/docs/auth/admin/verify-id-tokens) |
| Method | Post |
| Body | token (?) |
| Success Response | `{backend_token: string}` |

* User data sample:

```
export interface User {
    token: string;

    anonymous: boolean;

    name?: string;
    email?: string;
    email_verified?: boolean;
    photo_url?: string;
    
    created_at: Date;
    updated_at: Date;
}
```

* Notes

- If *verify-id-tokens* from Google doesn't return user data on the backend side, user data has to be provided as body of the request

- `backend_token` is the token to be provided in each guarded backend routes

### Logout

| <!-- -->    | <!-- -->    |
|-------------|-------------|
| Title | Logout |
| Description | Invalidate backend token |
| Method | Post |
| Body | token (?) |

* Notes

- should we also add a status on the user data and therefore set the status to `inactive` or else on logout?

