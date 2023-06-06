package com.unper.samper.model;

import lombok.*;


import javax.persistence.*;

import com.unper.samper.model.constant.ERole;



@Getter
@Setter
@Entity
@Table(name = "role")
@NoArgsConstructor
public class Role{
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    @Enumerated(EnumType.STRING)
    private ERole name;


}