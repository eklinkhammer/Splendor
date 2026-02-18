SSH into the Splendor deployment server for troubleshooting and configuration.

## Connection Details
- Command: `ssh -i ~/.ssh/EricMacRumble.pem ubuntu@207.5.207.232`
- User: ubuntu
- Server path: ~/Splendor

## Instructions

You have SSH access to the deployment server. Use this to:
- Troubleshoot server issues (check logs, processes, disk space, etc.)
- Read and write nginx configuration files
- Check docker container status and logs
- Inspect system state

### Common operations:
- View running containers: `ssh -i ~/.ssh/EricMacRumble.pem ubuntu@207.5.207.232 'docker ps'`
- Check server logs: `ssh -i ~/.ssh/EricMacRumble.pem ubuntu@207.5.207.232 'docker compose -f ~/Splendor/docker-compose.yml logs --tail=100'`
- Check nginx config: `ssh -i ~/.ssh/EricMacRumble.pem ubuntu@207.5.207.232 'cat /etc/nginx/sites-enabled/splendor'`
- Check disk space: `ssh -i ~/.ssh/EricMacRumble.pem ubuntu@207.5.207.232 'df -h'`

### For writing/editing files on the server:
Use `ssh -i ~/.ssh/EricMacRumble.pem ubuntu@207.5.207.232 'cat << "REMOTEEOF" > /path/to/file
file contents here
REMOTEEOF'`

### Important:
- Always confirm with the user before restarting services or making destructive changes
- When editing nginx configs, validate with `sudo nginx -t` before reloading with `sudo systemctl reload nginx`
- The Splendor app runs via docker compose at ~/Splendor
- Nginx runs as a host service (not in Docker). Config at `/etc/nginx/sites-enabled/splendor`
- Check nginx status: `sudo systemctl status nginx`
- View nginx logs: `sudo journalctl -u nginx --no-pager -n 100`

$ARGUMENTS
