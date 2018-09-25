#!/bin/bash
## Build and push new docker image to AWS ECR then pull and run new image in EC2 instance.
## Ensure AWS credentials are set in environment or the ansible playbook will fail to login to AWS

sudo -H pip install pip==18.0.0 \
    && sudo -H pip uninstall --yes setuptools \
    && sudo -H pip install 'setuptools<20.2' \
    && sudo -H pip install 'requests[security]' \
    && sudo -H pip install boto awscli ansible docker-py --upgrade \
    && ansible-playbook server/ansible/push-new-image.yml server/ansible/deploy-image.yml