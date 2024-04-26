from setuptools import setup

setup(
    name='Visual',
    version='0.1.0',
    author='Marco Bauce, Alessandro Riva',
    packages=['src/visual'],
    license='LICENSE.md',
    url='https://git.sr.ht/~heph/Italian-students-mental-disorders',
    description='An application that produce data visualization',
    long_description=open('README.md').read(),
    install_requires=[
        'pandas',
        'matplotlib'
    ],
)
