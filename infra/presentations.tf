### BUCKET PROPER

variable "presentations-bucket-name" {
  type    = string
  default = "deckdeckgo-beta-presentations"
}

variable "deckdeckgo-io-cert-arn" {
  type    = string
  default = "arn:aws:acm:us-east-1:610598948490:certificate/4a4693a2-5919-4012-9ea6-e657dcba377b"
}

resource "aws_s3_bucket" "presentations" {
  bucket = var.presentations-bucket-name

  website {
    index_document = "index.html"
  }

  policy = <<EOF
{
  "Version":"2012-10-17",
  "Statement":[{
    "Sid":"PublicReadForGetBucketObjects",
    "Effect":"Allow",
    "Principal": "*",
    "Action":["s3:GetObject"],
    "Resource":["arn:aws:s3:::${var.presentations-bucket-name}/*" ]
    }
  ]
}
EOF

}

output "presentations_endpoint" {
  value = aws_s3_bucket.presentations.website_endpoint
}

resource "aws_s3_bucket_object" "object" {
  bucket = var.presentations-bucket-name
  key = "index.html"
  source = "index.html"
  etag = filemd5("index.html")
  content_type = "text/html"

  depends_on = [aws_s3_bucket.presentations]
}

### CDN

resource "aws_cloudfront_origin_access_identity" "origin_access_identity" {
  comment = "cloudfront origin access identity"
}

resource "aws_cloudfront_distribution" "website_cdn" {
  enabled = true
  price_class = "PriceClass_200"
  http_version = "http1.1"
  aliases =  ["${aws_s3_bucket.presentations.bucket}.deckdeckgo.io", "beta.deckdeckgo.io"]

  origin {
    domain_name = aws_s3_bucket.presentations.website_endpoint
    origin_id = "origin-bucket-${aws_s3_bucket.presentations.id}"
    custom_origin_config {
      http_port = 80
      https_port = 443
      origin_protocol_policy = "http-only"
      origin_ssl_protocols = ["TLSv1.2"]
    }
  }

  default_root_object = "index.html"

  default_cache_behavior {
    allowed_methods = ["GET", "HEAD"]
    cached_methods = ["GET", "HEAD"]
    target_origin_id = "origin-bucket-${aws_s3_bucket.presentations.id}"

    min_ttl = "0"
    default_ttl = "300"
    max_ttl = "1200"

    viewer_protocol_policy = "redirect-to-https"
    compress = true

    forwarded_values {
      query_string = false

      cookies {
        forward = "none"
      }
    }
  }

  restrictions {
    geo_restriction {
      restriction_type = "none"
    }
  }

  viewer_certificate {
    acm_certificate_arn = var.deckdeckgo-io-cert-arn
    ssl_support_method = "sni-only"
  }
}

data "aws_route53_zone" "presentations" {
  name = "deckdeckgo.io"
}

resource "aws_route53_record" "www_site_beta" {
  zone_id = data.aws_route53_zone.presentations.zone_id
  name = "beta.deckdeckgo.io"
  type = "A"
  alias {
    name = aws_cloudfront_distribution.website_cdn.domain_name
    zone_id = aws_cloudfront_distribution.website_cdn.hosted_zone_id
    evaluate_target_health = false
  }
}

output "presentations_fqdn" {
  value = aws_route53_record.www_site_beta.fqdn
}

output "cloudfront_endpoint" {
  value = aws_cloudfront_distribution.website_cdn.domain_name
}

