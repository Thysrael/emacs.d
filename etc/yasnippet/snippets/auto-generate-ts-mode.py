import os

directory_path = './tree-sitter'

files = os.listdir(directory_path)

languages = []

for file in files:
    if file.startswith('libtree-sitter-') and file.endswith('.so'):
        language_name = file[len('libtree-sitter-'):-len('.so')]
        languages.append(language_name)

# 指定目标文件夹
target_folder = "./etc/yasnippet/snippets"

try:
    os.makedirs(target_folder)
    print(f"Folder '{target_folder}' created successfully.")
except FileExistsError:
    print(f"Folder '{target_folder}' already exists.")

for language in languages:
    folder_path = os.path.join(target_folder, f"{language}-ts-mode")
    try:
        os.makedirs(folder_path)
        print(f"Folder '{folder_path}' created successfully.")
    except:
        print(f"Folder {language}-mode already exists.")

    file_path = os.path.join(target_folder, f"{language}-ts-mode", ".yas-parents")
    with open(file_path, 'w') as file:
        file.write(language + "-mode")