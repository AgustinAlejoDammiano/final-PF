import React from "react";
import useStyles from "./styles";
import { Button, IconButton, Paper, Tooltip, Typography } from "@material-ui/core";
import HelpOutlineIcon from '@material-ui/icons/HelpOutline';

type Props = {
    onClick?: () => void,
    onClickTitle?: () => void,
    title?: string,
    icon?: React.ReactNode,
    titleVariant?: ("h1" | "h2" | "h3" | "h4" | "h5" | "h6"),
    explanation?: string,
    children: React.ReactNode
}

export default function Container({ onClick, onClickTitle, title, icon, titleVariant, explanation, children }: Props) {

    const classes = useStyles();

    return (
        <Paper className={classes.paper}>
            <div className={classes.titleRow}>
                <div className={classes.container}>
                    <Button className={classes.button} onClick={onClickTitle}>
                        <Typography variant={titleVariant ? titleVariant : "h5"}>{title}</Typography>
                    </Button>
                    {!!explanation &&
                        <Tooltip arrow classes={classes} title={`${explanation}`}>
                            <HelpOutlineIcon style={{width:"1rem", height:"1rem", marginLeft: "0.25rem"}}/>
                        </Tooltip>}
                </div>
                {icon &&
                    <IconButton className={classes.iconButton} onClick={onClick}>
                        {icon}
                    </IconButton>
                }
            </div>
            <div style={{width: "100%"}}>
                {children}
            </div>
        </Paper>
    )
}
