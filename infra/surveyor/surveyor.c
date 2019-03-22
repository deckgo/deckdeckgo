#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <dlfcn.h>
#include <string.h>
#include <arpa/inet.h>
#include <stdbool.h>
#include <netinet/in.h>

// TODO: check atoi returns

int (*super_connect)(int sockfd, const struct sockaddr *addr, socklen_t addrlen);

typedef struct
{
    // Port to map from. If "0" or "*" is given, maps from all ports.
    in_port_t port_from;

    // Port to map to. If "0" or "*" is given, maps to the original port.
    in_port_t port_to;

    // Address to map from. If "*" is given, maps from all addresses.
    struct in_addr *addr_from;

    // Address to map to. If "*" is given, maps to the original address.
    struct in_addr *addr_to;

} srv_mapping;


srv_mapping *mappings;
unsigned short n_mappings;

void _fini(void)
{
    if (mappings != NULL)
    {
        srv_mapping mapping;
        for (int i = 0; i < n_mappings; i ++)
        {
            // Free all the inner elements
            mapping = mappings[i];

            free(mapping.addr_from);
            free(mapping.addr_to);
        }

        free(mappings);

        mappings = NULL;
    }
}

void _init(void)
{
    char *mappings_str;
    if (!(mappings_str = getenv("SRV_MAP"))) {
        fprintf(stderr, "getenv(SRV_MAP) failed\n");
        exit(42);
    }

    fprintf(stderr, "%s\n", mappings_str);

    unsigned short mapping_strs_capacity = 1;
    char **mappings_strs = malloc(mapping_strs_capacity * sizeof(char*));;

    char *pch;
    pch = strtok(mappings_str, " ");

    while(pch != NULL)
    {
        fprintf(stderr, "Found mapping: %s\n", pch);

        if (n_mappings >= mapping_strs_capacity)
        {
            fprintf(stderr, "Growing ...\n");

            mapping_strs_capacity = 2 * mapping_strs_capacity;
            mappings_strs = realloc(mappings_strs, mapping_strs_capacity * sizeof(char*));
        }
        fprintf(stderr, "Copying to ix %d\n", n_mappings);

        mappings_strs[n_mappings] = malloc(strlen(pch));
        strcpy(mappings_strs[n_mappings], pch);
        n_mappings++;

        fprintf(stderr, "Next!\n");
        pch = strtok(NULL, " ");
    }

    fprintf(stderr, "Found %d mappings\n", n_mappings);

    mappings = malloc(n_mappings * sizeof(srv_mapping));

    for(int i = 0; i < n_mappings; i ++)
    {
        int res;

        fprintf(stderr, "Parsing mapping %d \n", n_mappings);

        pch = strtok(mappings_strs[i], ":");
        if(pch == NULL){ exit(43); }
        fprintf(stderr, "Parsing addr_from: %s\n", pch);

        if(strcmp(pch, "*") == 0)
        {
            mappings[i].addr_from = NULL;
        } else
        {
            mappings[i].addr_from = malloc(sizeof(struct in_addr));
            res = inet_pton(AF_INET, pch, mappings[i].addr_from);
            if (res == 1) {}
            else if (res == 0) { exit(43); }
            else if (res == -1) { exit(44); }
            else { exit(45); }
        }

        pch = strtok(NULL, ":");
        if(pch == NULL){ exit(43); }
        fprintf(stderr, "Parsing port_from: %s\n", pch);

        if(strcmp(pch, "0") == 0 || strcmp(pch, "*") == 0)
        {
            mappings[i].port_from = htons(0);
        } else
        {
            mappings[i].port_from = htons(atoi(pch));
        }


        pch = strtok(NULL, ":");
        if(pch == NULL){ exit(43); }

        fprintf(stderr, "Parsing addr_to: %s\n", pch);

        if(strcmp(pch, "*") == 0)
        {
            mappings[i].addr_to = NULL;
        } else
        {
            mappings[i].addr_to = malloc(sizeof(struct in_addr));
            res = inet_pton(AF_INET, pch, mappings[i].addr_to);
            if (res == 1) {}
            else if (res == 0) { exit(43); }
            else if (res == -1) { exit(44); }
            else { exit(45); }
        }

        pch = strtok(NULL, ":");
        if(pch == NULL){ exit(43); }

        fprintf(stderr, "Parsing port_to: %s\n", pch);

        if(strcmp(pch, "0") == 0 || strcmp(pch, "*") == 0)
        {
            mappings[i].port_to = htons(0);
        } else
        {
            mappings[i].port_to = htons(atoi(pch));
        }


        pch = strtok(NULL, ":");
        if(pch != NULL){ exit(12); }
    }

    const char *err;

    super_connect = dlsym(RTLD_NEXT, "connect");
    if ((err = dlerror()) != NULL) {
        fprintf(stderr, "dlsym(connect) failed: %s\n", err);
        exit(42);
    }
}

int connect(int sockfd, const struct sockaddr *dest_addr, socklen_t dest_len)
{

    fprintf(stderr, "Entering connect(2)\n");

    if (dest_addr->sa_family != AF_INET) {
        fprintf(stderr, "No AF_INET, skipping\n");
        return super_connect(sockfd, dest_addr, dest_len);
    }

    static struct sockaddr_in *dest_addr_in;
    dest_addr_in = (struct sockaddr_in *)dest_addr;

    bool found = false;
    srv_mapping mapping;

    for (int i = 0; i < n_mappings; i ++)
    {
        mapping = mappings[i];
        if (mapping.addr_from == NULL ||
            (*mapping.addr_from).s_addr == (dest_addr_in->sin_addr).s_addr)
        {
            if(mapping.port_from == 0 ||
                mapping.port_from == dest_addr_in->sin_port)
            {
                fprintf(stderr, "WE HAVE A MATCH!!!!!!\n");
                found = true;
                break;
            }
        }
    }

    if(found)
    {
        static struct sockaddr_storage new_dest_addr;
        memcpy(&new_dest_addr, dest_addr, dest_len);

        static struct sockaddr_in *new_dest_addr_in;
        new_dest_addr_in = (struct sockaddr_in *)&new_dest_addr;
        if(mapping.port_to != 0)
        {
            new_dest_addr_in->sin_port = mapping.port_to;
        }

        if(mapping.addr_to != NULL)
        {
            new_dest_addr_in->sin_addr = *mapping.addr_to;
        }
        return super_connect(sockfd, (struct sockaddr *)&new_dest_addr, dest_len);

    } else
    {
        return super_connect(sockfd, dest_addr, dest_len);

    }
}
