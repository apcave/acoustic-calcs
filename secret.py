import secrets

secret_key = ''.join(secrets.choice('abcdefghijklmnopqrstuvwxyz0123456789') for i in range(50))
print(secret_key)