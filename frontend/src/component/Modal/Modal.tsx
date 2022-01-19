import React from "react";
import useStyles from "./styles";
import { Dialog } from "@material-ui/core";
import CloseIcon from '@material-ui/icons/Close';
import Container from "../Container/Container";

type Props = {
    isOpen: boolean,
    onClose: () => void,
    onClickTitle?: () => void,
    title?: string,
    explanation?: string,
    children: React.ReactNode
}

export default function CustomModal({ isOpen, onClose, onClickTitle, title, explanation, children }: Props) {

    const classes = useStyles();

    return (
        <Dialog
            maxWidth="md"
            className={classes.modal}
            open={isOpen}
            onClose={onClose}
            PaperProps={{style: { backgroundColor: 'transparent', boxShadow: 'none', borderRadius: 20 }}}>
                <Container onClick={onClose} onClickTitle={onClickTitle} title={title} explanation={explanation} icon={<CloseIcon />}>
                    {children}
                </Container>
        </Dialog>
    )
}