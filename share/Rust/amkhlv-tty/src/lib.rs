
use termios::*;
use std::os::unix::io::{FromRawFd,IntoRawFd,RawFd};

pub fn setup(fd: RawFd) -> std::io::Result<()> {
    let mut termios = Termios::from_fd(fd)?;

    termios.c_iflag = IGNPAR | IGNBRK;
    termios.c_oflag = 0;
    termios.c_cflag = CS8 | CREAD | CLOCAL;
    termios.c_lflag = 0;

    cfsetspeed(&mut termios, B9600)?;
    tcsetattr(fd, TCSANOW, &termios)?;
    tcflush(fd, TCIOFLUSH)?;

    Ok(())
}
